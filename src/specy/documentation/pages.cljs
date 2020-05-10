(ns specy.documentation.pages
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.dom :as rdom]))


(defmulti ast->hiccup (fn [m] (condp = (:type m)
                                     "object" :object
                                     "array"  :array
                                     :default)))

(defmethod ast->hiccup :object [{:keys [required properties]}]
  (into [:div]
        (map (fn [[pname pschema]]
               [:div
                [:label {:className (if (contains? (set required) (some-> pname name)) "font-semibold inline-flex mr-3" "inline-flex mr-3")} pname]
                [:div {:className "inline-flex ml-8"} (ast->hiccup pschema)]])
             (sort-by #(not (contains? (set required) (some-> (first %) name))) properties))))


(defmethod ast->hiccup :array [{:keys [items]}]
  [:div
   [:label {:className "inline-flex mr-3"} "List of"]
   [:div {:className "inline-flex ml-8"} (ast->hiccup items)]])

(defmethod ast->hiccup :default [{:keys [type]}]
  [:div type])

#_(defmethod ast->hiccup :property [[pname _ value-type]]
  [:div {:className "group "}
   [:label {:className "inline-flex mr-3"} pname]
   [:div {:className "inline-flex"} value-type]])


#_(defmethod ast->hiccup :list [[_ _ value]]
  [:div
   [:label "list of"]
   [:div {:className "ml-8"}
    (ast->hiccup value)]])


#_(defmethod ast->hiccup :list [[_ _ value]]
  [:div
   [:label "list of"]
   [:div {:className "ml-8"}
    (ast->hiccup value)]])


#_(defmethod ast->hiccup :and [[_ _ & values]]
  [:div
   [:label "AND"]
   [:div {:className "ml-8"}
    (map ast->hiccup values)]])


#_(defmethod ast->hiccup :or [[_ _ & values]]
  [:div
   [:label "OR"]
   [:div {:className "ml-8"}
    (map ast->hiccup values)]])


#_(defmethod ast->hiccup :fn [[_ _ value]]
  [:div
   [:label "Rule"]
   [:div {:className "ml-8"}
    value]])

#_(defmethod ast->hiccup :enum [[_ _ & values]]
  (into [:div
         [:label "One of"]]
        (map (fn [v] [:div {:className "inline-flex ml-3"} v]) values)))


#_(defmethod ast->hiccup :nullable [[_ _ value]]
  [:div
   [:label {:className "inline-flex mr-3"} "Maybe"]
   (ast->hiccup value)])

(defn home-page [context]
  (prn context)
  [:div
   [:div.flex.overflow-hidden.h-screen.bg-gray-100
    ;; menu
    [:div.hidden.md:flex.md:flex-shrink-0
     [:div.flex.flex-col.w-64
      [:div.flex.items-center.h-16.flex-shrink-0.px-4.bg-gray-900
       [:img.h-8.w-auto {:src "/img/logos/workflow-logo-on-dark.svg" :alt "Workflow"}]]
      [:div.h-0.flex-1.flex.flex-col.overflow-y-auto
       [:nav.flex-1.px-2.py-4.bg-gray-800
        [:a.mt-1.group.flex.items-center.px-2.py-2.text-sm.leading-5.font-medium.text-gray-300.rounded-md.hover:text-white.hover:bg-gray-700.focus:outline-none.focus:text-white.focus:bg-gray-700.transition.ease-in-out.duration-150 {:href "#entities"}
         [:svg.mr-3.h-6.w-6.text-gray-300.group-hover:text-gray-300.group-focus:text-gray-300.transition.ease-in-out.duration-150 {:stroke "currentColor" :fill "none" :viewBox "0 0 24 24"}
          [:path {:stroke-linecap "round" :stroke-linejoin "round" :stroke-width "2" :d "M3 12l9-9 9 9M5 10v10a1 1 0 001 1h3a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1h3a1 1 0 001-1V10M9 21h6"}]] "Entities"]
        [:a.mt-1.group.flex.items-center.px-2.py-2.text-sm.leading-5.font-medium.text-gray-300.rounded-md.hover:text-white.hover:bg-gray-700.focus:outline-none.focus:text-white.focus:bg-gray-700.transition.ease-in-out.duration-150 {:href "#commands"}
         [:svg.mr-3.h-6.w-6.text-gray-400.group-hover:text-gray-300.group-focus:text-gray-300.transition.ease-in-out.duration-150 {:stroke "currentColor" :fill "none" :viewBox "0 0 24 24"}
          [:path {:stroke-linecap "round" :stroke-linejoin "round" :stroke-width "2" :d "M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z"}]] "Commands"]
        [:a.mt-1.group.flex.items-center.px-2.py-2.text-sm.leading-5.font-medium.text-gray-300.rounded-md.hover:text-white.hover:bg-gray-700.focus:outline-none.focus:text-white.focus:bg-gray-700.transition.ease-in-out.duration-150 {:href "#queries"}
         [:svg.mr-3.h-6.w-6.text-gray-400.group-hover:text-gray-300.group-focus:text-gray-300.transition.ease-in-out.duration-150 {:stroke "currentColor" :fill "none" :viewBox "0 0 24 24"}
          [:path {:stroke-linecap "round" :stroke-linejoin "round" :stroke-width "2" :d "M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-6l-2-2H5a2 2 0 00-2 2z"}]]
         "Queries"]
        [:a.mt-1.group.flex.items-center.px-2.py-2.text-sm.leading-5.font-medium.text-gray-300.rounded-md.hover:text-white.hover:bg-gray-700.focus:outline-none.focus:text-white.focus:bg-gray-700.transition.ease-in-out.duration-150 {:href "#events"}
         [:svg.mr-3.h-6.w-6.text-gray-400.group-hover:text-gray-300.group-focus:text-gray-300.transition.ease-in-out.duration-150 {:stroke "currentColor" :fill "none" :viewBox "0 0 24 24"}
          [:path {:stroke-linecap "round" :stroke-linejoin "round" :stroke-width "2" :d "M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z"}]] "Events"]
        [:a.mt-1.group.flex.items-center.px-2.py-2.text-sm.leading-5.font-medium.text-gray-300.rounded-md.hover:text-white.hover:bg-gray-700.focus:outline-none.focus:text-white.focus:bg-gray-700.transition.ease-in-out.duration-150 {:href "#"}
         [:svg.mr-3.h-6.w-6.text-gray-400.group-hover:text-gray-300.group-focus:text-gray-300.transition.ease-in-out.duration-150 {:stroke "currentColor" :fill "none" :viewBox "0 0 24 24"}
          [:path {:stroke-linecap "round" :stroke-linejoin "round" :stroke-width "2" :d "M20 13V6a2 2 0 00-2-2H6a2 2 0 00-2 2v7m16 0v5a2 2 0 01-2 2H6a2 2 0 01-2-2v-5m16 0h-2.586a1 1 0 00-.707.293l-2.414 2.414a1 1 0 01-.707.293h-3.172a1 1 0 01-.707-.293l-2.414-2.414A1 1 0 006.586 13H4"}]] "Scenarios"]]]]]

    ;; search bar
    [:div.flex.flex-col.w-0.flex-1.overflow-hidden
     [:div.relative.z-10.flex-shrink-0.flex.h-16.bg-white.shadow
      [:button.px-4.border-r.border-gray-200.text-gray-500.focus:outline-none.focus:bg-gray-100.focus:text-gray-600.md:hidden {:aria-label "Open sidebar"}
       [:svg.h-6.w-6 {:stroke "currentColor" :fill "none" :viewBox "0 0 24 24"}
        [:path {:stroke-linecap "round" :stroke-linejoin "round" :stroke-width "2" :d "M4 6h16M4 12h16M4 18h7"}]]]
      [:div.flex-1.px-4.flex.justify-between
       [:div.flex-1.flex
        [:div.w-full.flex.md:ml-0
         [:label.sr-only {:for "search_field"} "Search"]
         [:div.relative.w-full.text-gray-400.focus-within:text-gray-600
          [:div.absolute.inset-y-0.left-0.flex.items-center.pointer-events-none
           [:svg.h-5.w-5 {:fill "currentColor" :viewBox "0 0 20 20"}
            [:path {:fill-rule "evenodd" :clip-rule "evenodd" :d "M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z"}]]]
          [:input#search_field.block.w-full.h-full.pl-8.pr-3.py-2.rounded-md.text-gray-900.placeholder-gray-500.focus:outline-none.focus:placeholder-gray-400.sm:text-sm {:placeholder "Search" :type "search"}]]]]]]


     ;; content
     [:main.flex-1.relative.z-0.overflow-y-auto.py-6.focus:outline-none {:tabindex "0"}
      (into [:div
             [:div.max-w-7xl.mx-auto.px-4.sm:px-6.md:px-8
              [:h1.text-2xl.font-semibold.text-gray-900 [:a {:name "entities"} "Entities"]]]]
            (map (fn [{:keys [doc schema] :as entity}]
                   [:div.bg-white.px-4.py-5.border-b.border-gray-200.sm:px-6
                    [:h1.text-lg.leading-6.text-gray-900 {:class "font-semibold"}
                     [:a {:name (get entity :longname)} (get entity :name)]]
                    [:div.text-lg.leading-6.text-gray-900 {:class "mb-5"} doc]
                    [:h3.text-lg.leading-6.text-gray-900 {:class "font-semibold"} "Schema"]
                    (ast->hiccup schema)])
                 (filter #(= (:kind %) "entity") context)))

      (into [:div
             [:div.max-w-7xl.mx-auto.px-4.sm:px-6.md:px-8
              [:h1.text-2xl.font-semibold.text-gray-900 [:a {:name "values"} "Values"]]]]
            (map (fn [{:keys [doc schema] :as value}]
                   [:div.bg-white.px-4.py-5.border-b.border-gray-200.sm:px-6
                    [:h1.text-lg.leading-6.text-gray-900 {:class "font-semibold"} (get value :name)]
                    [:div.text-lg.leading-6.text-gray-900 {:class "mb-5"} doc]
                    [:h3.text-lg.leading-6.text-gray-900 {:class "font-semibold"} "Schema"]
                    (ast->hiccup schema)])
                 (filter #(= (:kind %) "value") context)))

      (into [:div
             [:div.max-w-7xl.mx-auto.px-4.sm:px-6.md:px-8
              [:h1.text-2xl.font-semibold.text-gray-900 [:a {:name "referentials"} "Referentials"]]]]
            (map (fn [{:keys [doc schema] :as referential}]
                   [:div.bg-white.px-4.py-5.border-b.border-gray-200.sm:px-6
                    [:h1.text-lg.leading-6.text-gray-900 {:class "font-semibold"} (get referential :name)]
                    [:div.text-lg.leading-6.text-gray-900 {:class "mb-5"} doc]
                    [:h3.text-lg.leading-6.text-gray-900 {:class "font-semibold"} "Schema"]
                    (ast->hiccup schema)])
                 (filter #(= (:kind %) "referential") context)))

      (into [:div
             [:div.max-w-7xl.mx-auto.px-4.sm:px-6.md:px-8
              [:h1.text-2xl.font-semibold.text-gray-900 [:a {:name "commands"} "Commands"]]]]
            (map (fn [{:keys [schema] :as command}]
                   [:div.bg-white.px-4.py-5.border-b.border-gray-200.sm:px-6
                    [:h1.text-lg.leading-6.text-gray-900 {:class "font-semibold"} (name (get command :name))]
                    [:div.text-lg.leading-6.text-gray-900 {:class "mb-5"} (get command :doc)]
                    [:div.leading-6.text-gray-900 {:class "mb-5"} "Rely on " [:a.italic.underline {:href (str "#" (get command :rely-on))} (get command :rely-on)]]
                    [:h3.text-lg.leading-6.text-gray-900 {:class "font-semibold"} "Schema"]
                    (ast->hiccup schema)])
                 (filter #(= (:kind %) "command") context)))

      (into [:div
             [:div.max-w-7xl.mx-auto.px-4.sm:px-6.md:px-8
              [:h1.text-2xl.font-semibold.text-gray-900 [:a {:name "queries"} "Queries"]]]]
            (map (fn [{:keys [schema] :as query}]
                   [:div.bg-white.px-4.py-5.border-b.border-gray-200.sm:px-6
                    [:h1.text-lg.leading-6.text-gray-900 {:class "font-semibold"} (name (get query :name))]
                    [:div.text-lg.leading-6.text-gray-900 {:class "mb-5"} (get query :doc)]
                    [:div.leading-6.text-gray-900 {:class "mb-5"} "Rely on " [:a.italic.underline {:href (str "#" (get query :rely-on))} (get query :rely-on)]]
                    [:h3.text-lg.leading-6.text-gray-900 {:class "font-semibold"} "Schema"]
                    (ast->hiccup schema)])
                 (filter #(= (:kind %) "query") context)))

      (into [:div
             [:div.max-w-7xl.mx-auto.px-4.sm:px-6.md:px-8
              [:h1.text-2xl.font-semibold.text-gray-900 [:a {:name "events"} "Events"]]]]
            (map (fn [{:keys [schema] :as event}]
                   [:div.bg-white.px-4.py-5.border-b.border-gray-200.sm:px-6
                    [:h1.text-lg.leading-6.text-gray-900 {:class "font-semibold"} (name (get event :name))]
                    [:div.text-lg.leading-6.text-gray-900 {:class "mb-5"} (get event :doc)]
                    [:div.leading-6.text-gray-900 {:class "mb-5"} "Rely on " [:a.italic.underline {:href (str "#" (get event :rely-on))} (get event :rely-on)]]
                    [:h3.text-lg.leading-6.text-gray-900 {:class "font-semibold"} "Schema"]
                    (ast->hiccup schema)])
                 (filter #(= (:kind %) "event") context)))

      ]]]])